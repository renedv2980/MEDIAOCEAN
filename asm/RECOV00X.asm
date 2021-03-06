*          DATA SET RECOV00X   AT LEVEL 013 AS OF 01/28/98                      
*PHASE T82300A,*                                                                
         TITLE 'RECOV00 - REP COVERSHEET EDITOR BASE'                           
***********************************************************************         
*                                                                     *         
*   RECOV00  (T82300)  UPDATE HISTORY                                 *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
* JAN28/98 RHV  IT'S ALIVE                                            *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T82300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82300*,RR=RE                                                 
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
         SPACE 1                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION CALL                                                 *         
***********************************************************************         
INIT     DS    0H                                                               
*                                                                               
* The initialisation call is performed every time the overlay is                
* loaded into the programs work area                                            
*                                                                               
* It is most useful for performing first time initialisation, such as           
* resolving data dictionary references and resetting local non-saved            
* flags.                                                                        
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS TO THE CONTROLLER                                       *         
* These are the standard return values that the controller requires   *         
* and understands                                                     *         
***********************************************************************         
         SPACE 1                                                                
* Normal exit points                                                            
*                                                                               
EXITH    CLI   *,0     cc high is understood to mean 'not known at              
         B     EXIT            this level' by the controller.                   
*                                                                               
EXITL    CLI   *,FF    cc low  is understood to mean 'validation error'         
         B     EXIT            by the controller                                
*                                                                               
EXITOK   CR    RB,RB   cc eq   is understood to mean 'all is ok' by the         
         B     EXIT            controller                                       
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
* Exit points after applying filtering (DFDO verb)                              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL   result of comaprison was LOW                     
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE   result of comparison was EQUAL                   
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH   result of comparison was HIGH                    
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX   result of comparison was that the record         
         B     EXITOK          is NOT WANTED in list                            
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
* This is a convenient way to loop round all the tables used to       *         
* define verbs within objects.                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         Table end?                                   
         BE    EXITH               Yes - set verb not known                     
*                                                                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB TO PROCESS                                    *         
***********************************************************************         
         SPACE 1                                                                
* In order that the controller knows that you want to provide some              
* processing for a particular object, it is first necessary to                  
* register it. This is performed by entering a line in the table of             
* known objects, below.                                                         
*                                                                               
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(OIO),AL1(0,0,0),AL4(IO)                                      
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFK)                                    
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(EXITH)                                
         DC    AL1(OREP),AL1(0,0,0),AL4(EXITH)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
***********************************************************************         
         SPACE 1                                                                
* The screen object allow the application to set which screen is the            
* correct one to be loaded for this particular case.                            
*                                                                               
SCREEN   LM    R1,R2,SVPARMS2                                                   
         LA    RF,SCRTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
SCRTABL  DC    AL1(SSET),AL1(0,0,0),AL4(SCRSET)                                 
         DC    AL1(SMOD),AL1(0,0,0),AL4(SCRMOD)                                 
         DC    AL1(SKSET),AL1(0,0,0),AL4(SCRKSET)                               
         DC    AL1(SKPRO),AL1(0,0,0),AL4(SCRKPRO)                               
         DC    AL1(SLSET),AL1(0,0,0),AL4(SCRLSET)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SET SCREEN CODE                                                     *         
*                                                                     *         
* NTRY: SVPARMS3   = A(KEY)                                           *         
* EXIT: GSSMCODE   = DATA SCREEN CODE TO SET                          *         
***********************************************************************         
         SPACE 1                                                                
SCRSET   DS    0H                                                               
*                                                                               
* Invoked only if 'Uses Screen Codes' flag is set on in Record record.          
*                                                                               
* Allows application to load in a different maintenance screen based            
* on information in the record key.                                             
*                                                                               
* Test whatever necessary in directory record.                                  
* Move into GSSMCODE whatever screen code is appropriate, or reset it           
* to zero for the default screen.                                               
* Based on setting, the correct data screen will be loaded in by the            
* controller                                                                    
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN MODIFY                                                       *         
* -------------                                                       *         
* SVPARMS3 B0    'SOLDQ' IF SCREEN HAS NOT BEEN LOADED THIS TIME      *         
* SVPARMS3 B0    'SNEWQ' IF SCREEN HAS BEEN LOADED THIS TIME          *         
* SVPARMS3 B1-3  EQUATED SCREEN NUMBER                                *         
***********************************************************************         
         SPACE 1                                                                
SCRMOD   DS    0H                                                               
*                                                                               
* Gives the application the opportunity to modify the appearance of a           
* screen, before any processing has been performed on it, based on              
* whether this is the first time it has been loaded, or not.                    
*                                                                               
* NOTE: The screen does not have a DSECT as with those which have been          
*       generated using PANGEN, so care should be taken when changing           
*       information based on position or displacement. Far better to            
*       obtain the field number and to decide based on that.                    
*                                                                               
* The following is one example of sample code which will pull out the           
* field element for a screen field. This contains the field number in           
* FDRNUM, which can then be used to decide what to do with the field.           
*                                                                               
* It is anticipated that this would be incorporated into a loop to go           
* down the screen until the end.                                                
*                                                                               
* It expects that you are using DDFH for the field header definitions.          
*                                                                               
* Remember that FHUS is in the extended field header, and as such is not        
* directly addressible.                                                         
*                                                                               
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BZ    NEXTFLD             NO - CANNOT BE ONE OF OURS THEN              
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    RF,FHD(RF)                                                       
         SH    RF,=Y(FHDAD)        RF = START OF EXTENDED FIELD HEADER          
         USING FHNU,RF                                                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHUS+1         FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    NEXTFLD             NOT A DATA FIELD IF THIS IS ZERO             
*                                                                               
         BCTR  RE,0                MAKE IT ZERO-BASED                           
         SLL   RE,2                                                             
         L     RF,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    RF,0(RE,RF)         INDEX INTO THE LIST OF FIELDS                
         ICM   RF,15,0(RF)                                                      
         USING FDRELD,RF                                                        
*                                                                               
*        APPLY REQUIRED TESTS & DO WHATEVER REQUIRED                            
*                                                                               
         B     NEXTFLD                                                          
*                                                                               
NEXTFLD  DS    0H                  CONTINUE LOOP                                
                                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY SCREEN CODE SET                                                 *         
*                                                                     *         
* EXIT: GSSKCODE   = KEY CODE TO SET                                  *         
***********************************************************************         
         SPACE 1                                                                
SCRKSET  DS    0H                                                               
*                                                                               
* Invoked only if 'Uses Key Codes' flags set on in Record record                
*                                                                               
* Allows application to load in a different key screen if required for          
* whatever reason. One example would be where different values would be         
* displayed if the application had been reached by an NTRSES rather than        
* by typing directly into the record/action fields                              
*                                                                               
* Test whatever necessary.                                                      
* Move into GSSKCODE whatever screen code is appropriate, or reset it           
* to zero (the default)                                                         
* The controller will load in the correct key screen as required.               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LIST HEADER SCREEN CODE SET                                         *         
*                                                                     *         
* EXIT: GSSLCODE   = LIST HEADER CODE TO SET                          *         
***********************************************************************         
         SPACE 1                                                                
SCRLSET  DS    0H                                                               
*                                                                               
* Invoked only if 'Uses List Codes' flag set on in Record record                
*                                                                               
* Allows application to load in a different list header if required for         
* whatever reason. One example would be where different values would be         
* displayed if the application had been reached by an NTRSES rather than        
* by typing directly into the record/action fields                              
*                                                                               
* Test whatever necessary.                                                      
* Move into GSSLCODE whatever screen code is appropriate, or reset it           
* to zero (the default)                                                         
* The controller will load in the correct list screen as required.              
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(KEY BUILD BUFFER)                                        *         
* SVPARMS4 HOLDS SUB-ACTION                                           *         
***********************************************************************         
         SPACE 1                                                                
* The key object deals with all the attributes of the whole directory           
* portion of a record, rather than the individual pieces of data which          
* are used to make the key unique for a particular record                       
*                                                                               
KEY      LM    R1,R2,SVPARMS2                                                   
         USING KEYDSECT,R2     <== DSECT for directory record                   
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(KMASK),AL1(0,0,0),AL4(KEYMASK)                               
         DC    AL1(KHEIR),AL1(0,0,0),AL4(KEYHEIR)                               
         DC    AL1(KLBUILD),AL1(0,0,0),AL4(KEYLBLD)                             
         DC    AL1(KMBUILD),AL1(0,0,0),AL4(KEYMBLD)                             
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
* Allows the user to perform any first time processing required before          
* the individual items of information within the record are processed.          
*                                                                               
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KFKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF KEY                                       *         
***********************************************************************         
         SPACE 1                                                                
KFKDIS   DS    0H                                                               
*                                                                               
* Allows any processing required before the individual items of data            
* within the key are displayed to be performed.                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF KEY                                      *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   DS    0H                                                               
*                                                                               
* This verb is invoked whenever a key needs to be built, before the             
* individual fields displayed on the current screen are processed.              
*                                                                               
* It allows one to set, for example, constants which identify the               
* directory record, or to change the default key fill (from X'00').             
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KFKFDIS  DS    0H                                                               
*                                                                               
* Allows any processing required to be performed by the application             
* before the controller attempts to display a list header screen from           
* the individual filters set on that screen.                                    
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  DS    0H                                                               
*                                                                               
* This verb is invoked whenever the controller needs to build an                
* initial key to begin listing records, from a list header screen,              
* before the 'key' level fields on that screen are processed.                   
*                                                                               
* As with the first time for validate verb, it allows one to set any            
* constants required to identify this key.                                      
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
* Similar in form and function to the first time for key object, but            
* all verbs are invoked after the current key processing has been               
* completed, before any other verbs are invoked.                                
*                                                                               
KEYLAST  L     R1,SVPARMS4         R3=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KLKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KLKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY OBJECT                               *         
***********************************************************************         
         SPACE 1                                                                
KLKDIS   DS    0H                                                               
*                                                                               
* This verb is invoked after the controller has finished displaying all         
* the fields which are defined as being at 'Key' level, on the current          
* screen.                                                                       
*                                                                               
* It enables the controller to provide any processing required at this          
* point                                                                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KLKVAL   DS    0H                                                               
*                                                                               
* This verb is invoked after the controller has finished validating all         
* the fields which are defined as being at 'Key' level, on the current          
* screen.                                                                       
*                                                                               
* It enables the controller to provide any processing required at this          
* point. Should dependant fields be onscreen, this is the best point to         
* perform their validation if the order is not sequential.                      
* i.e. A depends on B, A follows B on-screen  = sequential & can be             
*                                               processed in situ               
*      A depends on B, A precedes B on-screen = non -sequential. A must         
*                                               be saved until B has            
*                                               been validated.                 
*                                                                               
* Save the contents of FVADDR during the data minpulation, so that the          
* the cursor position can be reset in case of an input error.                   
* (Put the address into BOCURSOR to override).                                  
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY FILTER                               *         
***********************************************************************         
         SPACE 1                                                                
KLKFDIS  DS    0H                                                               
*                                                                               
* This verb is invoked after the controller has finished displaying all         
* the fields which are defined as being at 'Key' level, on the current          
* list header screen.                                                           
*                                                                               
* It enables the controller to provide any processing required at this          
* point                                                                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  DS    0H                                                               
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&TO HERE&&&&&&&&&&&&&&&&&&&&&&&&          
* This verb is invoked after the controller has finished validating all         
* the fields which are defined as being at 'Key' level, on the current          
* screen.                                                                       
*                                                                               
* It enables the controller to provide any processing required at this          
* point. Should dependant fields be onscreen, this is the best point to         
* perform their validation if the order is not sequential.                      
* i.e. A depends on B, A follows B on-screen  = sequential & can be             
*                                               processed in situ               
*      A depends on B, A precedes B on-screen = non -sequential. A must         
*                                               be saved until B has            
*                                               been validated.                 
*                                                                               
* Save the contents of FVADDR during the data minpulation, so that the          
* the cursor position can be reset in case of an input error.                   
* (Put the address into BOCURSOR to override).                                  
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET VALID ACTION MASK                                               *         
* ---------------------                                               *         
* SVPARMS3 = A(DIRECTORY RECORD KEY)                                  *         
* SVPARMS4 = A(MASK FOR THIS RECORD)                                  *         
* AIOREC   = A(RECORD IF ON FILE)                                     *         
* GSRECSTA = A(DIRECTORY STAUS AREA)                                  *         
* Allows the user to set valid actions for this particular record     *         
* based on the information set on the record. Default is to allow all *         
* actions. (Example of use: Not valid to delete an account which has  *         
* draft transactions - so turn off delete action flag valid)          *         
***********************************************************************         
         SPACE 1                                                                
KEYMASK  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* POPULATE HEIRARCHICAL RECORD                                        *         
* ----------------------------                                        *         
* If the record is set as being of heirarchical type (on the record   *         
* record), when adding a new record, this allows one to set the key   *         
* of a record which this record is to inherit from. If this record is *         
* on file, it is copied over into the empty record and displayed for  *         
* the user to edit as required.                                       *         
***********************************************************************         
         SPACE 1                                                                
KEYMASK  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD INITIAL KEY FOR NTRSES TRANSFER - LIST                        *         
* --------------------------------------------                        *         
* SVPARMS3 = A(KEY BUILD AREA)                                        *         
* SVPARMS4 = A(PSSAV)                                                 *         
* This verb allows one to build an initial key for listing from, when *         
* one session calls another.                                          *         
***********************************************************************         
         SPACE 1                                                                
KEYLBLD  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD INITIAL KEY FOR NTRSES TRANSFER - MAINTENANCE                 *         
* ---------------------------------------------------                 *         
* SVPARMS3 = A(KEY BUILD AREA)                                        *         
* SVPARMS4 = A(PSSAV)                                                 *         
* This verb allows one to build an initial key for listing from, on a *         
* maintenance list - IT IS NOT YET IMPLEMENTED                        *         
***********************************************************************         
         SPACE 1                                                                
KEYMBLD  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                       *         
* -------------                                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(RECORD)                                                  *         
* SVPARMS4 HOLDS SUB-ACTION VERB                                      *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R1,R3,SVPARMS2                                                   
         USING FDRRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RFDIS)                                  
         DC    AL1(RVAL),AL1(0,0,0),AL4(RFVAL)                                  
         DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DISPLAY                              *         
* --------------------------------------                              *         
* Allows one to set any first time processing required immediately    *         
* prior to the record associated with the current key being read into *         
* an io area ready to be displayed.                                   *         
***********************************************************************         
         SPACE 1                                                                
RFDIS    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - VALIDATE                             *         
* ---------------------------------------                             *         
* Allows one to set any first time processing required immediately    *         
* prior to the record associated with the current key being read into *         
* an io area ready to be validated.                                   *         
***********************************************************************         
         SPACE 1                                                                
RFVAL    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
* ----------------------------------                                  *         
* Allows one to set any first time processing required immediately    *         
* prior to the record associated with the current key being written   *         
* to the file                                                         *         
***********************************************************************         
         SPACE 1                                                                
RFADD    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
* -------------------------------------                               *         
* Allows one to set any first time processing required immediately    *         
* prior to the record associated with the current key being written   *         
* to the file with the delete status set.                             *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
* --------------------------------------                              *         
* Allows one to set any first time processing required immediately    *         
* prior to the record associated with the current key being written   *         
* to the file with the delete status removed.                         *         
***********************************************************************         
         SPACE 1                                                                
RFRES    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - WRITE TO FILE                        *         
* --------------------------------------                              *         
* Allows one to set any first time processing required immediately    *         
* prior to the record associated with the current key being written   *         
* to the file.                                                        *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
* ---------------------------                                         *         
* Identical in form and function to the first time for record object  *         
* except that it is invoked immediately after the io action has been  *         
* completed.                                                          *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RLDIS)                                  
         DC    AL1(RVAL),AL1(0,0,0),AL4(RLVAL)                                  
         DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DISPLAY                               *         
***********************************************************************         
         SPACE 1                                                                
RLDIS    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - VALIDATE                              *         
***********************************************************************         
         SPACE 1                                                                
RLVAL    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE 1                                                                
RLADD    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE 1                                                                
RLDEL    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE 1                                                                
RLRES    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE TO FILE                         *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
* -----------                                                         *         
* SVPARMS1 = EQUATED OBJECT IDENTIFIER                                *         
* SVPARMS2 = EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION       *         
* SVPARMS3 BYTE 0    = EQUATED DATA VERB IF SVPARMS2 IS ZERO          *         
* SVPARMS3 BYTES 1-3 = EQUATED ACTION VERB                            *         
* SVPARMS4 = A(RECORD AT CORRECT LEVEL)                               *         
* SVPARMS5 = A(FIELD TABLE ENTRY) OR ZERO IF SVPARMS2 IS ZERO OR IF   *         
*                                    ACTION IS DOWNLOAD               *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
* The data object provides functionality for dealing with all the     *         
* data associated with a record, wherever it may occur.               *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING FDRRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FDRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFDREL                                                        
         USING FDRELD,R3           R3=A(FDREL ON RECORD)                        
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(???DTA)    DATA OBJECT #1                         
         DC    AL2(00002),AL4(???DTA)    DATA OBJECT #2                         
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL20    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
*                                                                     *         
* PARAMETERS PASSED TO APPLICATION ARE HELD IN SVPARMS                *         
*                                                                     *         
* SVPARMS3 = VERB FOR WHICH THIS IS THE FIRST TIME CALL               *         
* SVPARMS4 = DATA AT CORRECT LEVEL - DEPENDING ON LEVEL SET IN THE    *         
*            FIELD RECORD ASSOCIATED WITH THIS PIECE OF DATA          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)    DISPLAY                      
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)    VALIDATE                     
         DC    AL1(DNTR),AL1(0,0,0),AL4(DFDNTR)    ENTER NEW SESSION            
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFDIS)  DISPLAY FILTER               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDFVAL)  VALIDATE FILTER              
         DC    AL1(DDISTOP)AL1(0,0,0),AL4(DFDTOP)  DISPLAY LIST TOP             
         DC    AL1(DDISBOT)AL1(0,0,0),AL4(DFDBOT)  DISPLAY LIST BOTTOM          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
* ---------------------------------------                             *         
* This verb is invoked after a record has been read from the file,    *         
* before any items of data from that record are displayed. It allows  *         
* one to get an element which holds several items of information and  *         
* save either the displacement to that element or a copy of the       *         
* element, rather than repeating the same element search code inside  *         
* each data object which is concerned with that element.              *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
* ---------------------------------------                             *         
* This verb is invoked after a record has been read from the file, or *         
* if adding, after an empty record has been created using the desired *         
* key, before any information has been validated from the screen and  *         
* placed into the record.                                             *         
* When adding, it allows one to create required elements, before any  *         
* code which expects the elements to exist is executed (An example of *         
* this is found on the account record - the RSTEL).                   *         
* It also allows one to copy elements which have mutually dependant   *         
* fields into working storage, and add the data for these fields      *         
* inside the data objects, then performing validation during the last *         
* time for data call. (This would only be required if the fields are  *         
* not on the screen in the order of their dependancy.)                *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA ON NTRSES                                       *         
* -----------------------------                                       *         
* This verb is invoked whenever the controller enters a session where *         
* the applications indicate that the caller wishes to pass parameters *         
* to the application it is calling.                                   *         
* This verb is applied to the caller application, and, as with the    *         
* first time for display verb, it is intended to allow addressing of  *         
* common elements.                                                    *         
***********************************************************************         
         SPACE 1                                                                
DFDNTR   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
* ---------------------------------------                             *         
* This verb is invoked whenever the controller is displaying filters  *         
* for a normal (non - maintenance screen) list. It is intended to be  *         
* used in a similar manner to the first time for display verb.        *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
* ----------------------------------------                            *         
* This verb is invoked whenever the controller is validating filters  *         
* for a normal (non - maintenance screen) list. It is intended to be  *         
* used in a similar manner to the first time for validate verb.       *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF DDISTOP                                   *         
* ---------------------------------                                   *         
* Allows one to set a line of totals brought forward onto the top of  *         
* the list screen if required.                                        *         
***********************************************************************         
         SPACE 1                                                                
DFDTOP   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF DDISBOT                                   *         
* ---------------------------------                                   *         
* Allows one to set a line of totals at the bottom of the list screen *         
* if required.                                                        *         
***********************************************************************         
         SPACE 1                                                                
DFDBOT   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
* -------------------------                                           *         
* Functionality offered is similar in form to that offered by the     *         
* first time for data object, but it is invoked after all data on the *         
* screen has been processed.                                          *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)    DISPLAY                      
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)    VALIDATE                     
         DC    AL1(DNTR),AL1(0,0,0),AL4(DLDNTR)    ENTER NEW SESSION            
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DLDFDIS)  DISPLAY FILTER               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DLDFVAL)  VALIDATE FILTER              
         DC    AL1(DDISTOP)AL1(0,0,0),AL4(DLDTOP)  DISPLAY LIST TOP             
         DC    AL1(DDISBOT)AL1(0,0,0),AL4(DLDBOT)  DISPLAY LIST BOTTOM          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR NTRSES                                                *         
***********************************************************************         
         SPACE 1                                                                
DLNTR    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
DLDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DLDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF TOP OF LIST SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
DLDTOP   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF BOTTOM OF LIST SCREEN                      *         
***********************************************************************         
         SPACE 1                                                                
DLDBOT   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR a piece of data                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
???DTA   LA    RF,???TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
???TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DIS???)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAL???)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HED???)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLT???)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLT???)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFT???)                                
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMH???)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DNTR???)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFL???)                               
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLF???)                              
         DC    AL1(DSRCH),AL1(0,0,0),AL4(DSCH???)                               
         DC    AL1(DDISTOP),AL1(0,0,0),AL4(DTOP???)                             
         DC    AL1(DDISBOT),AL1(0,0,0),AL4(DBOT???)                             
         DC    AL1(DRDIS),AL1(0,0,0),AL4(DRDI???)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A ?????? FIELD                                              *         
* ----------------------                                              *         
* Deals with the translation of the data from the internal            *         
* representation on the file into a form which is comprehensible to   *         
* the user on the screen.                                             *         
***********************************************************************         
         SPACE 1                                                                
DIS???   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A ?????? FIELD                                             *         
* -----------------------                                             *         
* Deals with the translation of the data from the representation on   *         
* the user screen into a form suitable for storage on the file.       *         
***********************************************************************         
         SPACE 1                                                                
VAL???   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY DYNAMIC LIST HEADLINES FOR ?????? FIELD                     *         
* -----------------------------------------------                     *         
* Allows one to change the column headings in the list to reflect the *         
* data which they are displaying.                                     *         
***********************************************************************         
         SPACE 1                                                                
HED???   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A ?????? FILTER FIELD                                       *         
* -----------------------------                                       *         
* This verb is used by the controller to build complicated filter     *         
* arrays based on the information set in the allowable filters on the *         
* field record. It is only necessary to code for the simplest input   *         
* i.e. the display of a solitary filter value.                        *         
***********************************************************************         
         SPACE 1                                                                
DFLT???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A ?????? FILTER FIELD                                      *         
* ------------------------------                                      *         
* This verb is used by the controller to validate complicated filter  *         
* arrays based on the information set in the allowable filters on the *         
* field record. It is only necessary to code for the simplest input   *         
* i.e. the validation of a solitary filter value.                     *         
***********************************************************************         
         SPACE 1                                                                
VFLT???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DO ?????? FILTERING                                                 *         
* -------------------                                                 *         
* This verb is used by the controller to validate whether a record is *         
* suitable for inclusion in the list based on the filters input by    *         
* user. If a record is DEFINATELY NOT WANTED for this list, go to the *         
* FLTXX label, otherwise the condition returned should allow the      *         
* controller to calculate the correct response.                       *         
***********************************************************************         
         SPACE 1                                                                
DOFT???  DS    0H                                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
*                                                                               
         B     FLTXX                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD DYNAMIC MAINTENANCE TAG FIELD                                 *         
* -----------------------------------                                 *         
* This verb allows one to change the tag associated with a field on a *         
* maintenance screen to reflect the data within the current record if *         
* this should be desired.                                             *         
***********************************************************************         
         SPACE 1                                                                
DMH???   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY FOR NTRSES                                                  *         
* ------------------                                                  *         
* If a calling application sets that it wishes to pass parameters to  *         
* the application it is calling, the caller is invoked with a DNTR    *         
* for every field which is flagged as being part of the key (if       *         
* calling a maintenance screen) or filterable (if calling a list).    *         
* The CALLER is expected to display any parameters that are desired   *         
* in a manner which is comprehensible to the CALLED application, as   *         
* if the screen of the called application were being validated        *         
* In most cases the code is the same as that for the DDIS verb.       *         
***********************************************************************         
         SPACE 1                                                                
DNTR???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SEARCH ON FIELD DATA                                                *         
* --------------------                                                *         
* This verb allows one to invoke search at an appropriate time if the *         
* data which it refers to is a searchable type.                       *         
***********************************************************************         
         SPACE 1                                                                
DSCH???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATA FOR TOP OF LIST SCREEN                                 *         
* -----------------------------------                                 *         
* This verb allows one to display a totals field on the top of a list *         
* screen                                                              *         
***********************************************************************         
         SPACE 1                                                                
DTOP???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATA FOR REPORT/DOWNLOAD                                    *         
* --------------------------------                                    *         
* This verb allows one to display data in a manner suitable for a     *         
* report/download.                                                    *         
***********************************************************************         
         SPACE 1                                                                
DRDI???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATA FOR BOTTOM OF LIST SCREEN                              *         
* --------------------------------------                              *         
* This verb allows one to display a totals field on the bottom of a   *         
* list screen.                                                        *         
***********************************************************************         
         SPACE 1                                                                
DBOT???  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS NEW RECORD ACTION                                          *         
* P4 BYTE  0   HOLDS ERROR CODE                                       *         
*    BYTES 1-3 HOLDS A(256 BYTE PARAMETER PASSING AREA)               *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R1,R3,SVPARMS2                                                   
         USING ???????,R2                                                       
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER TO NEXT LEVEL              *         
* ------------------------------------------------------              *         
* SVPARMS3 = A(NSSAV)                                                 *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM PREVIOUS LEVEL      *         
* --------------------------------------------------------------      *         
* SVPARMS3 = A(PSSAV)                                                 *         
* SVPARMS4 = A(FESD TO BE RESTORED)                                   *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER TO PREVIOUS LEVEL          *         
* ----------------------------------------------------------          *         
* SVPARMS3 = A(PSSAV)                                                 *         
* SVPARMS4 = A(FESD TO BE RESTORED)                                   *         
***********************************************************************         
         SPACE 1                                                                
XITOUT   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
* ------------------------------------------------------------        *         
* SVPARMS3 = A(NSSAV)                                                 *         
***********************************************************************         
         SPACE 1                                                                
XITIN    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
* -----------                                                         *         
* SVPARMS1 = EQUATED OBJECT                                           *         
* SVPARMS2 = EQUATED VERB                                             *         
* SVPARMS3 = CURRENT KEY BUILD AREA                                   *         
* SVPARMS4 = PREVIOUS KEY                                             *         
***********************************************************************         
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING ???????,R2                                                       
LAST     USING ???????,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LSCRFRST),AL1(0,0,0),AL4(FSCRLST)                            
         DC    AL1(LSCRLAST),AL1(0,0,0),AL4(LSCRLST)                            
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(LLSTLAST),AL1(0,0,0),AL4(LTFLST)                             
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(TSARFIL)                            
         DC    AL1(LTSARTSA),AL1(0,0,0),AL4(TSARTSA)                            
         DC    AL1(LUPDFRST),AL1(0,0,0),AL4(UPDFRST)                            
         DC    AL1(LUPDLAST),AL1(0,0,0),AL4(UPDLAST)                            
         DC    AL1(LUPDDIR),AL1(0,0,0),AL4(UPDDIR)                              
         DC    AL1(LUPDFIL),AL1(0,0,0),AL4(UPDFIL)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALISE LIST                                                     *         
* ---------------                                                     *         
* This verb is the first thing invoked before the list begins to be   *         
* processed. It allows one to set things such as the # of columns in  *         
* the list, etc.                                                      *         
***********************************************************************         
         SPACE 1                                                                
ILST     DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR LIST SCREEN                                          *         
* --------------------------                                          *         
* This verb is invoked before the current list screen is built. It    *         
* allows one to set any default processing required at this point.    *         
***********************************************************************         
         SPACE 1                                                                
FSCRLST  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR LIST SCREEN                                           *         
* -------------------------                                           *         
* This verb is invoked after the current list screen has been built   *         
* and populated. It allows one to set any default processing required *         
* at this point.                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSCRLST  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
* -------------------                                                 *         
* This verb is invoked before the list is built. It allows one to set *         
* any default processing required at this point.                      *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR LIST                                                  *         
* ------------------                                                  *         
* This verb is invoked after the list is built. It allows one to set  *         
* any default processing required at this point.                      *         
***********************************************************************         
         SPACE 1                                                                
LTFLST   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
* -------------------                                                 *         
* This verb is invoked at the top of each list page. It is intended   *         
* that the read sequence be refreshed here.                           *         
***********************************************************************         
         SPACE 1                                                                
FLST     DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NEXT FOR LIST                                                       *         
* -------------                                                       *         
* This verb is called repeatedly after the first for list page. It is *         
* intended that any records which meet the initial criteria be        *         
* selected here, where they may be filtered by the controller.        *         
***********************************************************************         
         SPACE 1                                                                
NLST     DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET TSAR RECORD FROM DIRECTORY                                      *         
* ------------------------------                                      *         
* This verb allows one to save information on the TSAR record from    *         
* the directory record.                                               *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET TSAR RECORD FROM FILE                                           *         
* -------------------------                                           *         
* This verb allows one to save information on the TSAR record from    *         
* the file record.                                                    *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ALLOW USER FILTERING                                                *         
* --------------------                                                *         
* This verb allows the user to test the filters set, and set          *         
* information on the tsar record based on this.                       *         
***********************************************************************         
         SPACE 1                                                                
TSARTSA  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR UPDATE                                                    *         
* ----------------                                                    *         
* This verb allows one to set default processing before all TSAR      *         
* are read during the update of a record from a maintenance list.     *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST FOR UPDATE                                                     *         
* ---------------                                                     *         
* This verb allows one to set default processing after all TSAR recs  *         
* are read during the update of a record from a maintenance list.     *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD                                        *         
* ----------------------------                                        *         
* This verb allows one to set information in the file record from a   *         
* TSAR record during the update from a maintenance list.              *         
***********************************************************************         
         SPACE 1                                                                
UPDFIL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD                                   *         
* ---------------------------------                                   *         
* This verb allows one to set information in the directory rec from a *         
* TSAR record during the update from a maintenance list.              *         
***********************************************************************         
         SPACE 1                                                                
UPDDIR   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE GEFILWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013RECOV00X  01/28/98'                                      
         END                                                                    
